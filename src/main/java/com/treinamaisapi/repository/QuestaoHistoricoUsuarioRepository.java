package com.treinamaisapi.repository;



import com.treinamaisapi.entity.questao_historico_usuario.QuestaoHistoricoUsuario;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import org.springframework.data.domain.Pageable;

import java.util.List;
import java.util.Map;

@Repository
public interface QuestaoHistoricoUsuarioRepository extends JpaRepository<QuestaoHistoricoUsuario, Long>, JpaSpecificationExecutor<QuestaoHistoricoUsuario>,  PagingAndSortingRepository<QuestaoHistoricoUsuario, Long>{
    // 1️⃣ Últimas questões respondidas
    @Query("SELECT qh.questao.id FROM QuestaoHistoricoUsuario qh " +
            "WHERE qh.usuario.id = :usuarioId ORDER BY qh.data DESC")
    List<Long> findUltimasQuestoesPorUsuario(@Param("usuarioId") Long usuarioId);

    // 2️⃣ Contagem de erros por questão
    @Query("SELECT qh.questao.id, COUNT(qh) FROM QuestaoHistoricoUsuario qh " +
            "WHERE qh.usuario.id = :usuarioId AND qh.acertou = false " +
            "GROUP BY qh.questao.id")
    List<Object[]> countErrosRawPorUsuario(@Param("usuarioId") Long usuarioId);

    // 3️⃣ Default method para Map<questaoId, quantidadeErros>
    default Map<Long, Long> countErrosPorUsuario(Long usuarioId) {
        List<Object[]> raw = countErrosRawPorUsuario(usuarioId);
        return raw.stream().collect(
                java.util.stream.Collectors.toMap(
                        r -> (Long) r[0],
                        r -> (Long) r[1]
                )
        );
    }


    // 🔹 Buscar IDs das questões que o usuário mais errou (ordenadas pela quantidade de erros)
    @Query("""
        SELECT qh.questao.id 
        FROM QuestaoHistoricoUsuario qh
        WHERE qh.usuario.id = :usuarioId 
          AND qh.acertou = false
        GROUP BY qh.questao.id
        ORDER BY COUNT(qh) DESC
    """)
    List<Long> findQuestoesMaisErradas(@Param("usuarioId") Long usuarioId, Pageable pageable);

    // (opcional) listar todas que o usuário errou, sem limite
    @Query("""
        SELECT DISTINCT qh.questao.id
        FROM QuestaoHistoricoUsuario qh
        WHERE qh.usuario.id = :usuarioId
          AND qh.acertou = false
    """)
    List<Long> findTodasQuestoesErradas(@Param("usuarioId") Long usuarioId);



}
