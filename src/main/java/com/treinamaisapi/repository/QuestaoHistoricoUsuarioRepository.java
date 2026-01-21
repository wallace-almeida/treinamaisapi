package com.treinamaisapi.repository;



import com.treinamaisapi.entity.questao_historico_usuario.QuestaoHistoricoUsuario;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import org.springframework.data.domain.Pageable;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

@Repository
public interface QuestaoHistoricoUsuarioRepository
        extends JpaRepository<QuestaoHistoricoUsuario, Long>,
        JpaSpecificationExecutor<QuestaoHistoricoUsuario> {

    // 1️⃣ Últimas questões respondidas (com paginação)
    @Query("""
    SELECT qh.questao.id
    FROM QuestaoHistoricoUsuario qh
    WHERE qh.usuario.id = :usuarioId
    ORDER BY qh.dataResposta DESC
""")
    List<Long> findUltimasQuestoesPorUsuario(
            @Param("usuarioId") Long usuarioId,
            org.springframework.data.domain.Pageable pageable
    );



    // 2️⃣ Contagem de erros por questão (RAW)
    @Query("""
        SELECT qh.questao.id, COUNT(qh)
        FROM QuestaoHistoricoUsuario qh
        WHERE qh.usuario.id = :usuarioId
          AND qh.acertou = false
        GROUP BY qh.questao.id
    """)
    List<Object[]> countErrosRawPorUsuario(@Param("usuarioId") Long usuarioId);

    // 3️⃣ Map<questaoId, quantidadeErros>
    default Map<Long, Long> countErrosPorUsuario(Long usuarioId) {
        return countErrosRawPorUsuario(usuarioId)
                .stream()
                .collect(Collectors.toMap(
                        r -> (Long) r[0],
                        r -> (Long) r[1]
                ));
    }

    // 4️⃣ Questões mais erradas (ordenadas)
    @Query("""
        SELECT qh.questao.id
        FROM QuestaoHistoricoUsuario qh
        WHERE qh.usuario.id = :usuarioId
          AND qh.acertou = false
        GROUP BY qh.questao.id
        ORDER BY COUNT(qh) DESC
    """)
    List<Long> findQuestoesMaisErradas(
            @Param("usuarioId") Long usuarioId,
            Pageable pageable
    );

    // 5️⃣ Todas as questões erradas
    @Query("""
        SELECT DISTINCT qh.questao.id
        FROM QuestaoHistoricoUsuario qh
        WHERE qh.usuario.id = :usuarioId
          AND qh.acertou = false
    """)
    List<Long> findTodasQuestoesErradas(@Param("usuarioId") Long usuarioId);

    // 6️⃣ Total de questões resolvidas
    Long countByUsuarioId(Long usuarioId);

    // 7️⃣ Total de acertos
    Long countByUsuarioIdAndAcertouTrue(Long usuarioId);

    @Query("""
        SELECT COUNT(DISTINCT DATE(q.dataResposta))
        FROM QuestaoHistoricoUsuario q
        WHERE q.usuario.id = :usuarioId
    """)
    Long countDiasAtivos(Long usuarioId);

    @Query("""
        SELECT 
            FUNCTION('DATE', q.dataResposta),
            SUM(CASE WHEN q.acertou = true THEN 1 ELSE 0 END) * 100.0 / COUNT(q)
        FROM QuestaoHistoricoUsuario q
        WHERE q.usuario.id = :usuarioId
          AND q.dataResposta >= :inicio
        GROUP BY FUNCTION('DATE', q.dataResposta)
        ORDER BY FUNCTION('DATE', q.dataResposta)
    """)
    List<Object[]> evolucaoAcertos(Long usuarioId, LocalDateTime inicio);

    @Query("""
        SELECT 
            q.temaNome,
            SUM(CASE WHEN q.acertou = true THEN 1 ELSE 0 END) * 100.0 / COUNT(q)
        FROM QuestaoHistoricoUsuario q
        WHERE q.usuario.id = :usuarioId
        GROUP BY q.temaNome
    """)
    List<Object[]> desempenhoPorMateria(Long usuarioId);
}