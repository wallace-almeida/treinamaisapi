package com.treinamaisapi.repository;


import com.treinamaisapi.entity.pontuacao.Pontuacao;
import com.treinamaisapi.entity.usuarios.Usuario;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public interface PontuacaoRepository extends JpaRepository<Pontuacao, Long>, JpaSpecificationExecutor<Pontuacao>,  PagingAndSortingRepository<Pontuacao, Long>{
    Optional<Pontuacao> findByUsuario(Usuario usuario);





}
