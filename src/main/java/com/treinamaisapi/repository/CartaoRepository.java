package com.treinamaisapi.repository;



import com.treinamaisapi.common.dto.flashcarddashboard.FlashcardBaralhoResumo;
import com.treinamaisapi.entity.cartao.Cartao;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

@Repository
public interface CartaoRepository extends JpaRepository<Cartao, Long>, JpaSpecificationExecutor<Cartao>,  PagingAndSortingRepository<Cartao, Long>{



    Optional<Cartao> findByUsuarioIdAndId(Long usuarioId, Long id);

    boolean existsByUsuarioIdAndQuestaoId(Long usuarioId, Long questaoId);

    @Query("""
        SELECT COUNT(c)
        FROM Cartao c
        WHERE c.usuario.id = :usuarioId
          AND (c.proximaRevisao IS NULL OR c.proximaRevisao <= CURRENT_TIMESTAMP)
    """)
    int contarPendentesHoje(Long usuarioId);

    @Query("""
        SELECT COUNT(c)
        FROM Cartao c
        WHERE c.usuario.id = :usuarioId
          AND c.ultimaRevisao IS NOT NULL
          AND DATE(c.ultimaRevisao) = CURRENT_DATE
    """)
    int contarRevisadosHoje(Long usuarioId);


    @Query("""
    SELECT c FROM Cartao c
    WHERE c.usuario.id = :usuarioId
      AND (c.proximaRevisao IS NULL OR c.proximaRevisao <= :data)
    ORDER BY c.proximaRevisao ASC
""")
    List<Cartao> buscarProximoParaEstudo(Long usuarioId, LocalDateTime data, Pageable pageable);




}
