package com.treinamaisapi.repository;


import com.treinamaisapi.common.filtroAuxil.FiltroArvoreLinhaProjection;
import com.treinamaisapi.common.filtroAuxil.PacoteHeaderProjection;
import com.treinamaisapi.entity.pacotes.Pacote;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface PacoteFiltroRepository extends JpaRepository<Pacote, Long>, JpaSpecificationExecutor<Pacote>,  PagingAndSortingRepository<Pacote, Long>{
    @Query("""
        select
            p.id as pacoteId,
            p.nome as nomePacote,
            p.versao as versao,
            c.id as concursoId,
            c.nome as nomeConcurso
        from Pacote p
        join p.concurso c
        where p.id in :pacoteIds
        order by p.id
    """)
    List<PacoteHeaderProjection> listarHeaders(@Param("pacoteIds") List<Long> pacoteIds);

    @Query("""
        select
            p.id as pacoteId,
            t.id as temaId,
            t.nome as temaNome,
            c.id as capituloId,
            c.nome as capituloNome,
            s.id as subcapituloId,
            s.nome as subcapituloNome
        from Pacote p
        join p.temas t
        join t.capitulos c
        join c.subcapitulos s
        where p.id in :pacoteIds
        order by p.id, t.nome, c.nome, s.nome
    """)
    List<FiltroArvoreLinhaProjection> listarArvoreFiltros(@Param("pacoteIds") List<Long> pacoteIds);

}
