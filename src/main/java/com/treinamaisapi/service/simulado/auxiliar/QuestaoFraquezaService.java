package com.treinamaisapi.service.simulado.auxiliar;

import com.treinamaisapi.entity.questoes.Questao;
import com.treinamaisapi.entity.usuarios.Usuario;
import com.treinamaisapi.repository.QuestaoHistoricoUsuarioRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class QuestaoFraquezaService {

    private final QuestaoHistoricoUsuarioRepository historicoRepo;

    public List<Questao> buscarQuestoesDeFraqueza(Usuario usuario, List<Questao> questoesDisponiveis, int limite) {
        List<Long> idsFraquezas = historicoRepo.findQuestoesMaisErradas(usuario.getId(), PageRequest.of(0, limite));

        return questoesDisponiveis.stream()
                .filter(q -> idsFraquezas.contains(q.getId()))
                .limit(limite)
                .collect(Collectors.toList());
    }
}
