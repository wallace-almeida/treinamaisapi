package com.treinamaisapi.service.simulado.auxiliar;


import com.treinamaisapi.common.dto.simulado.request.CriarSimuladoRequest;
import com.treinamaisapi.entity.questoes.Questao;
import com.treinamaisapi.entity.usuarios.Usuario;
import com.treinamaisapi.repository.QuestaoHistoricoUsuarioRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.List;

@Service
@RequiredArgsConstructor
public class QuestaoHistoricoService {


    private final QuestaoHistoricoUsuarioRepository historicoRepo;

    public List<Questao> filtrarNaoRespondidas(Usuario usuario, List<Questao> questoes) {
        List<Long> respondidasRecentes = historicoRepo.findUltimasQuestoesPorUsuario(usuario.getId());
        return questoes.stream()
                .filter(q -> !respondidasRecentes.contains(q.getId()))
                .toList();
    }

}

