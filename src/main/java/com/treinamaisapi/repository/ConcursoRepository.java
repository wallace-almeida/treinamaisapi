package com.treinamaisapi.repository;


import com.treinamaisapi.entity.Concurso;
import com.treinamaisapi.entity.pacotes.Pacote;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public interface ConcursoRepository extends JpaRepository<Concurso, Long>, JpaSpecificationExecutor<Concurso>,  PagingAndSortingRepository<Concurso, Long>{
    Optional<Concurso> findByNomeIgnoreCase(String nome);


}
