package com.treinamaisapi.repository;


import com.treinamaisapi.entity.capitulo.Capitulo;
import com.treinamaisapi.entity.pacotes.Pacote;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public interface PacoteRepository extends JpaRepository<Pacote, Long>, JpaSpecificationExecutor<Pacote>,  PagingAndSortingRepository<Pacote, Long>{
    boolean existsByNomeAndConcursoId(String nome, Long concursoId);


}
