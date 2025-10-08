package com.autosenseapi.repository.proprietario;


import com.autosenseapi.entity.usuarios.proprietario.UsuarioProprietario;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;

import org.springframework.data.repository.PagingAndSortingRepository;



import org.springframework.stereotype.Repository;

@Repository
public interface UsuarioProprietarioRepository extends JpaRepository<UsuarioProprietario, Long>, JpaSpecificationExecutor<UsuarioProprietario>,  PagingAndSortingRepository<UsuarioProprietario, Long>{





}
