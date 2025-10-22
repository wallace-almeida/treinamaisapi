package com.treinamaisapi.repository;


import com.treinamaisapi.entity.simulado.Simulado;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface SimuladoRepository extends JpaRepository<Simulado, Long>, JpaSpecificationExecutor<Simulado>,  PagingAndSortingRepository<Simulado, Long>{

    List<Simulado> findByUsuarioId(Long usuarioId);


}
