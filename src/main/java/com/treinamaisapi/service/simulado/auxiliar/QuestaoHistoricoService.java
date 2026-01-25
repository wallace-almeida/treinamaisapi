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

@Service
@RequiredArgsConstructor
@Slf4j
public class QuestaoHistoricoService {

    private final QuestaoHistoricoUsuarioRepository historicoRepo;

    // ✅ Quantidade de questões recentes que NÃO queremos repetir
    private static final int LIMITE_QUESTOES_RECENTES = 50;

    /**
     * Remove do conjunto as questões que o usuário respondeu mais recentemente.
     * Não banimos pra sempre, apenas evitamos repetir as últimas N.
     */
    public List<Long> filtrarIdsNaoRecentes(Usuario usuario, List<Long> ids) {

        Long usuarioId = usuario.getId();

        if (ids == null || ids.isEmpty()) return List.of();

        List<Long> respondidasRecentes = historicoRepo.findUltimasQuestoesPorUsuario(
                usuarioId,
                PageRequest.of(0, LIMITE_QUESTOES_RECENTES)
        );

        Set<Long> recentesSet = new HashSet<>(respondidasRecentes);

        List<Long> filtradas = ids.stream()
                .filter(Objects::nonNull)
                .filter(id -> !recentesSet.contains(id))
                .distinct()
                .toList();

        log.debug("[HISTÓRICO] filtrarIdsNaoRecentes. usuarioId={}, antes={}, recentes={}, depois={}",
                usuarioId, ids.size(), respondidasRecentes.size(), filtradas.size());

        return filtradas;
    }



}
