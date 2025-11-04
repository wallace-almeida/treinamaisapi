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
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface QuestaoRepository extends JpaRepository<Questao, Long>, JpaSpecificationExecutor<Questao>,  PagingAndSortingRepository<Questao, Long>{

    @Query("""
        SELECT q FROM Questao q
        JOIN q.subcapitulo sc
        JOIN sc.capitulo c
        JOIN c.tema t
        WHERE (:temaIds IS NULL OR t.id IN (:temaIds))
          AND (:capituloIds IS NULL OR c.id IN (:capituloIds))
          AND (:subcapituloIds IS NULL OR sc.id IN (:subcapituloIds))
          AND (:nivel IS NULL OR q.nivelDificuldade = :nivel)
          AND (:banca IS NULL OR q.banca = :banca)
    """)
    List<Questao> buscarPorFiltros(
            @Param("temaIds") List<Long> temaIds,
            @Param("capituloIds") List<Long> capituloIds,
            @Param("subcapituloIds") List<Long> subcapituloIds,
            @Param("nivel") NivelDificuldade nivel,
            @Param("banca") String banca,
            Pageable pageable
    );




}
