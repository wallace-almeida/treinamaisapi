package com.treinamaisapi.repository;

import com.treinamaisapi.entity.cartao.Cartao;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

@Repository
public interface CartaoRepository extends JpaRepository<Cartao, Long>, JpaSpecificationExecutor<Cartao> {

    Optional<Cartao> findByUsuarioIdAndId(Long usuarioId, Long id);

    boolean existsByUsuarioIdAndQuestaoId(Long usuarioId, Long questaoId);

    @Query("""
        SELECT COUNT(c)
        FROM Cartao c
        WHERE c.usuario.id = :usuarioId
          AND (c.proximaRevisao IS NULL OR c.proximaRevisao <= CURRENT_TIMESTAMP)
    """)
    Long contarPendentesHoje(@Param("usuarioId") Long usuarioId);

    @Query("""
        SELECT COUNT(c)
        FROM Cartao c
        WHERE c.usuario.id = :usuarioId
          AND c.ultimaRevisao >= :inicio
          AND c.ultimaRevisao < :fim
    """)
    Long contarRevisadosHoje(@Param("usuarioId") Long usuarioId,
                             @Param("inicio") LocalDateTime inicio,
                             @Param("fim") LocalDateTime fim);

    @Query("""
        SELECT c FROM Cartao c
        WHERE c.usuario.id = :usuarioId
          AND (c.proximaRevisao IS NULL OR c.proximaRevisao <= :data)
        ORDER BY c.proximaRevisao ASC
    """)
    List<Cartao> buscarProximoParaEstudo(@Param("usuarioId") Long usuarioId,
                                         @Param("data") LocalDateTime data,
                                         Pageable pageable);
}
