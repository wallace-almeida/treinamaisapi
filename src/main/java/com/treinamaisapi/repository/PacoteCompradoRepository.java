package com.treinamaisapi.repository;


import com.treinamaisapi.entity.pacotes.Pacote;
import com.treinamaisapi.entity.pacotes.PacoteComprado;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface PacoteCompradoRepository extends JpaRepository<PacoteComprado, Long>, JpaSpecificationExecutor<PacoteComprado>,  PagingAndSortingRepository<PacoteComprado, Long>{
    @Query("""
        SELECT pc FROM PacoteComprado pc
        WHERE pc.usuario.id = :usuarioId
        AND pc.ativo = true
        AND pc.dataExpiracao > CURRENT_TIMESTAMP
    """)
    List<PacoteComprado> findAllAtivosByUsuario(Long usuarioId);

    @Query("""
        SELECT pc FROM PacoteComprado pc
        WHERE pc.usuario.id = :usuarioId
        AND pc.pacote.id = :pacoteId
        AND pc.ativo = true
        AND pc.dataExpiracao > CURRENT_TIMESTAMP
    """)
    Optional<PacoteComprado> findAtivoENaoExpirado(Long usuarioId, Long pacoteId);

    List<PacoteComprado> findByUsuarioIdAndAtivoTrue(Long usuarioId);

    boolean existsByUsuarioIdAndPacoteIdAndAtivoTrue(Long usuarioId, Long pacoteId);
}
