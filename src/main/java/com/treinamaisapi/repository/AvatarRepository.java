package com.treinamaisapi.repository;


import com.treinamaisapi.entity.avatar.Avatar;
import com.treinamaisapi.entity.usuarios.Usuario;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface AvatarRepository extends JpaRepository<Avatar, Long>, JpaSpecificationExecutor<Avatar>,  PagingAndSortingRepository<Avatar, Long>{
    List<Avatar> findByAtivoTrue();

    Optional<Avatar> findByNomeAndAtivoTrue(String nome);




}
