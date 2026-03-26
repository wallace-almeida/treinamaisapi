package com.treinamaisapi.repository;


import com.treinamaisapi.entity.enums.pacotes.MeioPagamento;
import com.treinamaisapi.entity.enums.pacotes.StatusCompra;
import com.treinamaisapi.entity.pacotes.Pacote;
import com.treinamaisapi.entity.pacotes.PacoteComprado;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

@Repository
public interface PacoteCompradoRepository extends JpaRepository<PacoteComprado, Long>, JpaSpecificationExecutor<PacoteComprado>,  PagingAndSortingRepository<PacoteComprado, Long>{


    Optional<PacoteComprado> findByUsuarioIdAndPacoteId(Long usuarioId, Long pacoteId);


    List<PacoteComprado> findByUsuarioIdAndStatusAndDataExpiracaoAfter(
            Long usuarioId,
            StatusCompra status,
            LocalDateTime agora
    );


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

    Optional<PacoteComprado> findTopByUsuarioIdAndPacoteIdAndMeioPagamentoAndStatusInOrderByIdDesc(
            Long usuarioId,
            Long pacoteId,
            MeioPagamento meioPagamento,
            List<StatusCompra> status
    );


    Optional<PacoteComprado> findByPixTxId(String pixTxId);

    @Query("""
    SELECT pc.pacote.id
    FROM PacoteComprado pc
    WHERE pc.usuario.id = :usuarioId
      AND pc.status = :status
      AND pc.dataExpiracao > :agora
""")
    List<Long> findPacoteIdsComAcessoAtivo(
            @Param("usuarioId") Long usuarioId,
            @Param("status") StatusCompra status,
            @Param("agora") LocalDateTime agora
    );

    @Modifying(clearAutomatically = true)
    @Query("""
UPDATE PacoteComprado pc
   SET pc.ativo = false
 WHERE pc.ativo = true
   AND pc.dataExpiracao <= :agora
""")
    int expirarPacotes(@Param("agora") LocalDateTime agora);
}
