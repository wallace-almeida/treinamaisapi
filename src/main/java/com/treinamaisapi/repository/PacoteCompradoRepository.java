package com.treinamaisapi.repository;


import com.treinamaisapi.entity.enums.pacotes.StatusCompra;
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


    Optional<PacoteComprado> findByUsuarioIdAndPacoteId(Long usuarioId, Long pacoteId);


    List<PacoteComprado> findByUsuarioIdAndAtivoTrue(Long usuarioId);


    Optional<PacoteComprado> findByIdAndUsuarioId(Long id, Long usuarioId);



    //novos

    boolean existsByUsuarioIdAndPacoteIdAndStatusAndAtivoTrue(
            Long usuarioId,
            Long pacoteId,
            StatusCompra status
    );

    List<PacoteComprado> findByUsuarioIdAndPacoteIdAndStatus(
            Long usuarioId,
            Long pacoteId,
            StatusCompra status
    );

    @Query("""
select pc.pacote.id
from PacoteComprado pc
where pc.usuario.id = :usuarioId
  and pc.ativo = true
  and pc.status = :status
""")
    List<Long> findPacoteIdsAtivosByUsuarioAndStatus(Long usuarioId, StatusCompra status);


    Optional<PacoteComprado> findByPixTxId(String pixTxId);
}
