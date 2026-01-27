package com.treinamaisapi.service.simulado.auxiliar;

import com.treinamaisapi.common.dto.simulado.request.CriarSimuladoRequest;
import com.treinamaisapi.common.filtroAuxil.QuestaoNivelProjection;
import com.treinamaisapi.entity.enums.NivelDificuldade;
import com.treinamaisapi.entity.questoes.Questao;
import com.treinamaisapi.repository.QuestaoRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.stream.Collectors;

@Service
@Slf4j
@RequiredArgsConstructor
public class QuestaoBalanceService {

    private final QuestaoRepository questaoRepository;

    public List<Long> balancearIds(List<Long> ids, CriarSimuladoRequest request, int quantidadeTotal) {

        if (ids == null || ids.isEmpty() || quantidadeTotal <= 0) return List.of();

        // 0) Sanitiza entrada
        List<Long> base = ids.stream()
                .filter(Objects::nonNull)
                .distinct()
                .toList();

        if (base.isEmpty()) return List.of();

        // 1) Parse níveis selecionados (obrigatório)
        List<NivelDificuldade> selecionados = parseNiveisObrigatorios(request);
        if (selecionados.isEmpty()) {
            throw new IllegalArgumentException("Níveis são obrigatórios para balanceamento.");
        }

        // 2) Busca níveis em lote
        List<QuestaoNivelProjection> rows = questaoRepository.findNiveisByIds(base);

        Map<Long, NivelDificuldade> nivelPorId = new HashMap<>();
        for (QuestaoNivelProjection r : rows) {
            if (r.getId() != null) nivelPorId.put(r.getId(), r.getNivel()); // pode ser null
        }

        // 3) Buckets apenas dos níveis selecionados (preserva a “aleatoriedade” da ordem em base)
        Map<NivelDificuldade, List<Long>> bucket = new LinkedHashMap<>();
        for (NivelDificuldade n : selecionados) bucket.put(n, new ArrayList<>());

        List<Long> semNivel = new ArrayList<>();

        for (Long id : base) {
            NivelDificuldade n = nivelPorId.get(id);
            if (n == null) {
                semNivel.add(id);
                continue;
            }
            List<Long> b = bucket.get(n);
            if (b != null) b.add(id); // só entra se for nível selecionado
        }

        // 4) Metas: distribuição uniforme entre níveis selecionados
        // Ex.: 50 e 3 níveis -> 17,17,16
        Map<NivelDificuldade, Integer> meta = distribuirUniforme(quantidadeTotal, selecionados);

        // 5) Seleciona até meta por nível
        List<Long> selecionadas = new ArrayList<>(quantidadeTotal);
        Set<Long> usados = new HashSet<>();

        Map<NivelDificuldade, Integer> faltas = new LinkedHashMap<>();
        for (NivelDificuldade n : selecionados) {
            int alvo = meta.getOrDefault(n, 0);
            int pegou = adicionarAte(selecionadas, usados, bucket.get(n), alvo);
            int faltou = alvo - pegou;
            if (faltou > 0) faltas.put(n, faltou);
        }

        // 6) Redistribui o que faltou para níveis selecionados com sobra
        // Monta uma lista “sobra” com o restante dos buckets selecionados
        if (selecionadas.size() < quantidadeTotal) {
            List<Long> sobra = new ArrayList<>();
            for (NivelDificuldade n : selecionados) {
                // adiciona tudo do bucket (vamos proteger por "usados")
                sobra.addAll(bucket.get(n));
            }
            adicionarAte(selecionadas, usados, sobra, quantidadeTotal - selecionadas.size());
        }

        // 7) (Opcional) Se você quiser permitir completar com "semNivel" para não ficar abaixo:
        // Se preferir ser rígido (apenas níveis selecionados), REMOVA esse bloco e lance exceção se faltar.
        if (selecionadas.size() < quantidadeTotal) {
            adicionarAte(selecionadas, usados, semNivel, quantidadeTotal - selecionadas.size());
        }

        // 8) Garantia final: não passa do total e embaralha
        if (selecionadas.size() > quantidadeTotal) {
            selecionadas = selecionadas.subList(0, quantidadeTotal);
        }

        Collections.shuffle(selecionadas);

        log.debug("[BALANCE] entradaBase={}, selecionados={}, bucketSizes={}, semNivel={}, saida={}",
                base.size(),
                selecionados,
                bucketSizes(bucket),
                semNivel.size(),
                selecionadas.size()
        );

        return selecionadas;
    }

    // ----------------- helpers -----------------

    private List<NivelDificuldade> parseNiveisObrigatorios(CriarSimuladoRequest request) {
        if (request == null || request.getNiveis() == null) return List.of();

        List<NivelDificuldade> out = new ArrayList<>();
        for (String s : request.getNiveis()) {
            if (s == null) continue;
            String v = s.trim().toUpperCase();
            if (v.isBlank()) continue;
            try {
                out.add(NivelDificuldade.valueOf(v));
            } catch (Exception ignored) {
                // se vier valor inválido, ignore (ou lance exceção se preferir)
            }
        }

        // remove duplicados preservando ordem
        return out.stream().distinct().toList();
    }

    private Map<NivelDificuldade, Integer> distribuirUniforme(int total, List<NivelDificuldade> niveis) {
        Map<NivelDificuldade, Integer> meta = new LinkedHashMap<>();
        int k = niveis.size();
        int base = total / k;
        int resto = total % k;

        for (int i = 0; i < k; i++) {
            meta.put(niveis.get(i), base + (i < resto ? 1 : 0));
        }
        return meta;
    }

    private int adicionarAte(List<Long> out, Set<Long> usados, List<Long> fonte, int limite) {
        if (limite <= 0 || fonte == null || fonte.isEmpty()) return 0;
        int antes = out.size();
        for (Long id : fonte) {
            if (out.size() >= antes + limite) break;
            if (id != null && usados.add(id)) out.add(id);
        }
        return out.size() - antes;
    }

    private Map<NivelDificuldade, Integer> bucketSizes(Map<NivelDificuldade, List<Long>> bucket) {
        Map<NivelDificuldade, Integer> m = new LinkedHashMap<>();
        for (var e : bucket.entrySet()) {
            m.put(e.getKey(), e.getValue() == null ? 0 : e.getValue().size());
        }
        return m;
    }
}
