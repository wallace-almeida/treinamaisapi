package com.treinamaisapi.repository;


import com.treinamaisapi.entity.capitulo.Capitulo;
import com.treinamaisapi.entity.subCapitulo.Subcapitulo;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface SubCapituloRepository extends JpaRepository<Subcapitulo, Long>, JpaSpecificationExecutor<Subcapitulo>,  PagingAndSortingRepository<Subcapitulo, Long>{




}
