package com.treinamaisapi.repository;


import com.treinamaisapi.entity.tema.Tema;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface TemaRepository extends JpaRepository<Tema, Long>, JpaSpecificationExecutor<Tema>,  PagingAndSortingRepository<Tema, Long>{

    boolean existsByNomeIgnoreCase(String nome);
    Optional<Tema> findByNomeIgnoreCase(String nome);
    List<Tema> findByNomeInIgnoreCase(List<String> nomes);


}
