package com.treinamaisapi.repository;


import com.treinamaisapi.entity.avatar.Avatar;
import com.treinamaisapi.entity.resetSenha.ResetSenhaToken;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface ResetSenhaTokenRepository extends JpaRepository<ResetSenhaToken, Long>, JpaSpecificationExecutor<ResetSenhaToken>,  PagingAndSortingRepository<ResetSenhaToken, Long>{

    Optional<ResetSenhaToken> findByUsuarioEmailAndCodigoAndUsadoFalse(
            String email, String codigo);

    Optional<ResetSenhaToken> findByTokenAndUsadoFalse(String token);



}
