package com.treinamaisapi.repository;


import com.treinamaisapi.entity.capitulo.Capitulo;
import com.treinamaisapi.entity.enums.NivelDificuldade;
import com.treinamaisapi.entity.questoes.Questao;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface QuestaoRepository extends JpaRepository<Questao, Long>, JpaSpecificationExecutor<Questao>,  PagingAndSortingRepository<Questao, Long>{

    @Query("""
        SELECT q FROM Questao q
        WHERE (:temaId IS NULL OR q.subcapitulo.capitulo.tema.id = :temaId)
          AND (:capituloId IS NULL OR q.subcapitulo.capitulo.id = :capituloId)
          AND (:subcapituloId IS NULL OR q.subcapitulo.id = :subcapituloId)
          AND (:nivelDificuldade IS NULL OR q.nivelDificuldade = :nivelDificuldade)
          AND (:banca IS NULL OR q.banca = :banca)
        ORDER BY function('RAND')
        """)
    List<Questao> buscarPorFiltros(Long temaId,
                                   Long capituloId,
                                   Long subcapituloId,
                                   NivelDificuldade nivelDificuldade,
                                   String banca,
                                   Pageable pageable);

    default List<Questao> buscarPorFiltros(Long temaId,
                                           Long capituloId,
                                           Long subcapituloId,
                                           NivelDificuldade nivelDificuldade,
                                           String banca,
                                           Integer quantidade) {
        return buscarPorFiltros(
                temaId, capituloId, subcapituloId, nivelDificuldade, banca,
                PageRequest.of(0, quantidade)
        );
    }


}
