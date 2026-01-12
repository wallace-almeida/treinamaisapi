package com.treinamaisapi.repository;


import com.treinamaisapi.entity.enums.StatusSimulado;
import com.treinamaisapi.entity.simulado.Simulado;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

@Repository
public interface SimuladoRepository extends JpaRepository<Simulado, Long>, JpaSpecificationExecutor<Simulado>,  PagingAndSortingRepository<Simulado, Long>{

    List<Simulado> findByUsuarioIdOrderByDataCriacaoDesc(Long usuarioId);

    Optional<Simulado> findFirstByUsuarioIdAndStatus(Long usuarioId, StatusSimulado status);

    @Query("""
        SELECT COALESCE(SUM(
            FUNCTION('TIMESTAMPDIFF', MINUTE, s.dataCriacao, s.dataFinalizacao)
        ), 0)
        FROM Simulado s
        WHERE s.usuario.id = :usuarioId
          AND s.status = 'FINALIZADO'
          AND s.dataFinalizacao IS NOT NULL
    """)
    Long sumTempoEstudoByUsuario(@Param("usuarioId") Long usuarioId);

    List<Simulado> findByUsuarioIdAndStatusAndDataCriacaoAfterOrderByDataCriacaoDesc(Long usuarioId,StatusSimulado status, LocalDateTime data);

}
