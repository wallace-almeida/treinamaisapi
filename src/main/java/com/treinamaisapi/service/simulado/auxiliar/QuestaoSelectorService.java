package com.treinamaisapi.service.simulado.auxiliar;


import com.treinamaisapi.common.dto.simulado.request.CriarSimuladoRequest;
import com.treinamaisapi.entity.questoes.Questao;
import com.treinamaisapi.entity.usuarios.Usuario;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class QuestaoSelectorService {


    public List<Questao> selecionar(List<Questao> questoes, Usuario usuario, int quantidade, CriarSimuladoRequest request) {
        // garante que trabalhamos com lista mutável e sem duplicatas
        List<Questao> pool = new ArrayList<>(questoes.stream()
                .distinct()
                .collect(Collectors.toList()));

        Collections.shuffle(pool);

        return pool.stream()
                .limit(quantidade)
                .collect(Collectors.toList());
    }
}

