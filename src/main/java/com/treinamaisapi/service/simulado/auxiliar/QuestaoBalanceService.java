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
public class QuestaoBalanceService {


    public List<Questao> balancear(List<Questao> questoes, CriarSimuladoRequest request) {

        List<Questao> faceis = new ArrayList<>();
        List<Questao> medios = new ArrayList<>();
        List<Questao> dificeis = new ArrayList<>();

        for (Questao q : questoes) {
            switch (q.getNivelDificuldade()) {
                case FACIL -> faceis.add(q);
                case MEDIO -> medios.add(q);
                case DIFICIL -> dificeis.add(q);
            }
        }

        // Exemplo simples: 30% fácil, 50% médio, 20% difícil
        int total = questoes.size();
        List<Questao> balanceadas = new ArrayList<>();
        balanceadas.addAll(faceis.stream().limit(total * 30 / 100).collect(Collectors.toList()));
        balanceadas.addAll(medios.stream().limit(total * 50 / 100).collect(Collectors.toList()));
        balanceadas.addAll(dificeis.stream().limit(total * 20 / 100).collect(Collectors.toList()));

        // Preenche com restantes se faltar
        List<Questao> restantes = new ArrayList<>(questoes);
        while (balanceadas.size() < total && !restantes.isEmpty()) {
            balanceadas.add(restantes.remove(0));
        }


        Collections.shuffle(balanceadas);
        return balanceadas;
    }

}

