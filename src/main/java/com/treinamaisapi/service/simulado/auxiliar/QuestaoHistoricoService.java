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
    public List<Questao> filtrarNaoRespondidas(Usuario usuario, List<Questao> questoes) {

        Long usuarioId = usuario.getId();
        log.debug("[HISTÓRICO] Iniciando filtro de não respondidas. usuarioId={}, totalQuestoesEntrada={}",
                usuarioId, questoes.size());

        // Busca só as últimas N questões respondidas
        List<Long> respondidasRecentes = historicoRepo.findUltimasQuestoesPorUsuario(
                usuarioId,
                PageRequest.of(0, LIMITE_QUESTOES_RECENTES)
        );

        log.debug("[HISTÓRICO] Últimas questões respondidas encontradas. usuarioId={}, totalRespondidasRecentes={}, ids={}",
                usuarioId, respondidasRecentes.size(), resumirIds(respondidasRecentes));

        Set<Long> respondidasSet = new HashSet<>(respondidasRecentes);

        List<Questao> filtradas = questoes.stream()
                .filter(q -> q.getId() != null && !respondidasSet.contains(q.getId()))
                .toList();

        log.debug("[HISTÓRICO] Resultado do filtro. usuarioId={}, antes={}, depois={}",
                usuarioId, questoes.size(), filtradas.size());

        return filtradas;
    }

    // Só pra não poluir log com lista gigante
    private String resumirIds(List<Long> ids) {
        int max = Math.min(ids.size(), 10);
        return ids.stream()
                .limit(max)
                .toList()
                .toString() + (ids.size() > max ? " ... (+" + (ids.size() - max) + ")" : "");
    }
}
