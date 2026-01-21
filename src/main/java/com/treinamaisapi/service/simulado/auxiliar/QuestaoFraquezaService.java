package com.treinamaisapi.service.simulado.auxiliar;

import com.treinamaisapi.entity.questoes.Questao;
import com.treinamaisapi.entity.usuarios.Usuario;
import com.treinamaisapi.repository.QuestaoHistoricoUsuarioRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Slf4j
public class QuestaoFraquezaService {

    private final QuestaoHistoricoUsuarioRepository historicoRepo;

    /**
     * Retorna questões do pool disponível que estão entre as mais erradas do usuário.
     */
    public List<Questao> buscarQuestoesDeFraqueza(Usuario usuario,
                                                  List<Questao> questoesDisponiveis,
                                                  int limite) {

        Long usuarioId = usuario.getId();
        log.debug("[FRAQUEZA] Iniciando busca de fraquezas. usuarioId={}, totalPool={}, limite={}",
                usuarioId, questoesDisponiveis.size(), limite);

        // Busca ids das questões mais erradas (global para o usuário, depois filtramos pelo pool)
        List<Long> idsFraquezas = historicoRepo.findQuestoesMaisErradas(
                usuarioId,
                PageRequest.of(0, limite)
        );

        log.debug("[FRAQUEZA] IDs de fraquezas retornados do histórico. usuarioId={}, totalIds={}, ids={}",
                usuarioId, idsFraquezas.size(), resumirIds(idsFraquezas));

        Set<Long> fraquezasSet = new HashSet<>(idsFraquezas);

        List<Questao> selecionadas = questoesDisponiveis.stream()
                .filter(q -> q.getId() != null && fraquezasSet.contains(q.getId()))
                .limit(limite)
                .collect(Collectors.toList());

        log.debug("[FRAQUEZA] Resultado da filtragem no pool. usuarioId={}, totalSelecionadas={}",
                usuarioId, selecionadas.size());

        return selecionadas;
    }

    private String resumirIds(List<Long> ids) {
        int max = Math.min(ids.size(), 10);
        return ids.stream()
                .limit(max)
                .toList()
                .toString() + (ids.size() > max ? " ... (+" + (ids.size() - max) + ")" : "");
    }
}
