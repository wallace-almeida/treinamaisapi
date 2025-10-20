package com.treinamaisapi.repository;


import com.treinamaisapi.entity.usuarios.Usuario;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public interface UsuarioRepository extends JpaRepository<Usuario, Long>, JpaSpecificationExecutor<Usuario>,  PagingAndSortingRepository<Usuario, Long>{
    Optional<Usuario> findByEmail(String email);
    boolean existsByEmail(String email);





}
