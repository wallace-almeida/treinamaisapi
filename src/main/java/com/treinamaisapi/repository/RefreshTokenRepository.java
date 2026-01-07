package com.treinamaisapi.repository;


import com.treinamaisapi.entity.refreshToken.RefreshToken;
import com.treinamaisapi.entity.usuarios.Usuario;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public interface RefreshTokenRepository extends JpaRepository<RefreshToken, Long>, JpaSpecificationExecutor<RefreshToken>,  PagingAndSortingRepository<RefreshToken, Long>{
    Optional<RefreshToken> findByToken(String token);

    @Modifying
    @Query("update RefreshToken r set r.revogado = true where r.usuario.id = :usuarioId")
    void revokeAllByUsuario(@Param("usuarioId") Long usuarioId);



}
