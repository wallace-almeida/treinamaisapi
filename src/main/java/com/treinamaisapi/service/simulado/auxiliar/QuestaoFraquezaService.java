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
import java.util.Objects;
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
    public List<Long> buscarIdsDeFraqueza(Usuario usuario, List<Long> poolIds, int limite) {

        Long usuarioId = usuario.getId();

        if (poolIds == null || poolIds.isEmpty() || limite <= 0) return List.of();

        // ids das mais erradas do usuário (top N)
        List<Long> idsFraquezas = historicoRepo.findQuestoesMaisErradas(
                usuarioId,
                PageRequest.of(0, limite)
        );

        Set<Long> poolSet = new HashSet<>(poolIds);

        List<Long> filtradas = idsFraquezas.stream()
                .filter(Objects::nonNull)
                .filter(poolSet::contains)   // pega só as que existem no pool atual
                .distinct()
                .limit(limite)
                .toList();

        log.debug("[FRAQUEZA] buscarIdsDeFraqueza. usuarioId={}, pool={}, idsFraquezas={}, retorno={}",
                usuarioId, poolIds.size(), idsFraquezas.size(), filtradas.size());

        return filtradas;
    }

}
