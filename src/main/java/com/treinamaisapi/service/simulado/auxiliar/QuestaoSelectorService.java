package com.treinamaisapi.service.simulado.auxiliar;

import com.treinamaisapi.common.dto.simulado.request.CriarSimuladoRequest;
import com.treinamaisapi.entity.questoes.Questao;
import com.treinamaisapi.entity.usuarios.Usuario;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Slf4j
public class QuestaoSelectorService {

    public List<Long> selecionarIds(List<Long> poolIds, int limite) {
        if (poolIds == null || poolIds.isEmpty() || limite <= 0) return List.of();

        // copia mutável
        List<Long> copia = new ArrayList<>(poolIds);
        // embaralha (barato, rápido)
        Collections.shuffle(copia);

        List<Long> selecionadas = copia.stream()
                .filter(Objects::nonNull)
                .distinct()
                .limit(limite)
                .toList();

        log.debug("[SELECTOR] selecionarIds. pool={}, limite={}, selecionadas={}",
                poolIds.size(), limite, selecionadas.size());

        return selecionadas;
    }
}
