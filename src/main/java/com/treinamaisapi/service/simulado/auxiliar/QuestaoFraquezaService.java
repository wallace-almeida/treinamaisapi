package com.treinamaisapi.service.simulado.auxiliar;


import com.treinamaisapi.entity.questoes.Questao;
import com.treinamaisapi.entity.usuarios.Usuario;
import com.treinamaisapi.repository.QuestaoHistoricoUsuarioRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Map;

@Service
@RequiredArgsConstructor
public class QuestaoFraquezaService {


    private final QuestaoHistoricoUsuarioRepository historicoRepo;

    public List<Questao> priorizarFraquezas(Usuario usuario, List<Questao> questoes) {

        // Exemplo: questões que o usuário errou mais vezes aparecem primeiro
        Map<Long, Long> errosPorQuestao = historicoRepo.countErrosPorUsuario(usuario.getId());

        return questoes.stream()
                .sorted((q1, q2) -> Long.compare(errosPorQuestao.getOrDefault(q2.getId(), 0L),
                        errosPorQuestao.getOrDefault(q1.getId(), 0L)))
                .toList();
    }

}

