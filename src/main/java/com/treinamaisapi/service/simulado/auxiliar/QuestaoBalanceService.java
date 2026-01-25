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

        // 1) busca níveis em lote
        List<QuestaoNivelProjection> rows = questaoRepository.findNiveisByIds(ids);

        // id -> nivel
        Map<Long, NivelDificuldade> nivelPorId = new HashMap<>();
        for (QuestaoNivelProjection r : rows) {
            if (r.getId() != null && r.getNivel() != null) {
                nivelPorId.put(r.getId(), r.getNivel());
            }
        }

        List<Long> faceis = new ArrayList<>();
        List<Long> medios = new ArrayList<>();
        List<Long> dificeis = new ArrayList<>();
        List<Long> semNivel = new ArrayList<>();

        for (Long id : ids) {
            NivelDificuldade nivel = nivelPorId.get(id);
            if (nivel == null) {
                semNivel.add(id);
                continue;
            }
            switch (nivel) {
                case FACIL -> faceis.add(id);
                case MEDIO -> medios.add(id);
                case DIFICIL -> dificeis.add(id);
            }
        }

        // 2) proporção padrão: 30/50/20 (pode virar config depois)
        int qtdFaceis = quantidadeTotal * 30 / 100;
        int qtdMedios = quantidadeTotal * 50 / 100;
        int qtdDificeis = quantidadeTotal * 20 / 100;

        List<Long> selecionadas = new ArrayList<>();

        selecionadas.addAll(faceis.stream().distinct().limit(qtdFaceis).toList());
        selecionadas.addAll(medios.stream().distinct().limit(qtdMedios).toList());
        selecionadas.addAll(dificeis.stream().distinct().limit(qtdDificeis).toList());

        // 3) completa se faltou (sem repetir)
        Set<Long> usados = new HashSet<>(selecionadas);

        // prioridade: ainda tentar pegar de categorias com sobra
        List<Long> resto = new ArrayList<>();
        resto.addAll(faceis);
        resto.addAll(medios);
        resto.addAll(dificeis);
        resto.addAll(semNivel);

        for (Long id : resto) {
            if (selecionadas.size() >= quantidadeTotal) break;
            if (id != null && usados.add(id)) selecionadas.add(id);
        }

        // 4) embaralha
        Collections.shuffle(selecionadas);

        log.debug("[BALANCE] balancearIds. entrada={}, saida={}, faceis={}, medios={}, dificeis={}, semNivel={}",
                ids.size(), selecionadas.size(), faceis.size(), medios.size(), dificeis.size(), semNivel.size());

        return selecionadas.stream().limit(quantidadeTotal).toList();
    }
}
