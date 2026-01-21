package com.treinamaisapi.service.simulado.auxiliar;

import com.treinamaisapi.common.dto.simulado.request.CriarSimuladoRequest;
import com.treinamaisapi.entity.enums.NivelDificuldade;
import com.treinamaisapi.entity.questoes.Questao;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

@Service
@Slf4j
public class QuestaoBalanceService {

    public List<Questao> balancear(List<Questao> questoes, CriarSimuladoRequest request) {

        log.debug("[BALANCE] Iniciando balanceamento. totalEntrada={}", questoes.size());

        List<Questao> faceis = new ArrayList<>();
        List<Questao> medios = new ArrayList<>();
        List<Questao> dificeis = new ArrayList<>();

        for (Questao q : questoes) {
            NivelDificuldade nivel = q.getNivelDificuldade();
            if (nivel == null) continue;

            switch (nivel) {
                case FACIL -> faceis.add(q);
                case MEDIO -> medios.add(q);
                case DIFICIL -> dificeis.add(q);
            }
        }

        log.debug("[BALANCE] Distribuição inicial por dificuldade: faceis={}, medios={}, dificeis={}",
                faceis.size(), medios.size(), dificeis.size());

        // Estratégia padrão (pode virar config no futuro):
        // 30% fácil, 50% médio, 20% difícil
        int total = questoes.size();

        int qtdFaceis = total * 30 / 100;
        int qtdMedios = total * 50 / 100;
        int qtdDificeis = total * 20 / 100;

        log.debug("[BALANCE] Tamanhos desejados -> faceis={}, medios={}, dificeis={}",
                qtdFaceis, qtdMedios, qtdDificeis);

        List<Questao> balanceadas = new ArrayList<>();
        balanceadas.addAll(faceis.stream().limit(qtdFaceis).collect(Collectors.toList()));
        balanceadas.addAll(medios.stream().limit(qtdMedios).collect(Collectors.toList()));
        balanceadas.addAll(dificeis.stream().limit(qtdDificeis).collect(Collectors.toList()));

        // Remove duplicadas caso alguma categoria não tenha quantidade suficiente
        balanceadas = balanceadas.stream().distinct().collect(Collectors.toList());

        log.debug("[BALANCE] Após seleção por dificuldade (sem duplicatas). totalParcial={}",
                balanceadas.size());

        // Preenche com restantes se faltar (sem repetir)
        List<Questao> restantes = new ArrayList<>(questoes);
        restantes.removeAll(balanceadas);

        while (balanceadas.size() < total && !restantes.isEmpty()) {
            balanceadas.add(restantes.remove(0));
        }

        log.debug("[BALANCE] Após preencher com restantes. totalFinalAntesShuffle={}",
                balanceadas.size());

        Collections.shuffle(balanceadas);

        log.debug("[BALANCE] Balanceamento finalizado. totalSaida={}", balanceadas.size());

        return balanceadas;
    }
}
