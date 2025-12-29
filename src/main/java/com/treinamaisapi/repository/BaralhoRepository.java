package com.treinamaisapi.repository;


import com.treinamaisapi.common.dto.flashcarddashboard.FlashcardBaralhoResumo;
import com.treinamaisapi.entity.baralho.Baralho;
import com.treinamaisapi.entity.usuarios.Usuario;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface BaralhoRepository extends JpaRepository<Baralho, Long>, JpaSpecificationExecutor<Baralho>,  PagingAndSortingRepository<Baralho, Long>{
    Optional<Baralho> findByUsuarioIdAndTemaId(Long usuarioId, Long temaId);


    @Query("""
SELECT 
    b.id AS id,
    b.titulo AS titulo,
    b.tema.nome AS temaNome,
    COUNT(c.id) AS totalCartoes,
    COALESCE(SUM(
        CASE WHEN c.proximaRevisao IS NULL 
              OR c.proximaRevisao <= CURRENT_TIMESTAMP
             THEN 1 ELSE 0 END
    ), 0) AS pendentesHoje
FROM Baralho b
LEFT JOIN b.cartoes c
WHERE b.usuario.id = :usuarioId
GROUP BY b.id, b.titulo, b.tema.nome
ORDER BY b.titulo
""")
    List<FlashcardBaralhoResumo> listarResumo(Long usuarioId);





}
