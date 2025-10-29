package com.treinamaisapi.repository;


import com.treinamaisapi.entity.capitulo.Capitulo;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public interface CapituloRepository extends JpaRepository<Capitulo, Long>, JpaSpecificationExecutor<Capitulo>,  PagingAndSortingRepository<Capitulo, Long>{
    Optional<Capitulo> findByNomeIgnoreCaseAndTema_Id(String nome, Long temaId);




}
