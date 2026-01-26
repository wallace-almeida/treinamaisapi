package com.treinamaisapi.repository;


import com.treinamaisapi.entity.refreshToken.RefreshToken;
import com.treinamaisapi.entity.usuarios.Usuario;
import jakarta.persistence.LockModeType;
import org.springframework.data.jpa.repository.*;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

@Repository
public interface RefreshTokenRepository extends JpaRepository<RefreshToken, Long>, JpaSpecificationExecutor<RefreshToken>{
    Optional<RefreshToken> findByToken(String token);

    @Modifying
    @Query("update RefreshToken r set r.revogado = true where r.usuario.id = :usuarioId and r.revogado = false")
    int revokeAllByUsuario(@Param("usuarioId") Long usuarioId);


    List<RefreshToken> findAllByUsuarioAndRevogadoFalse(Usuario usuario);

    // 🔒 trava a linha do refresh token durante o refresh (rotation)
    @Lock(LockModeType.PESSIMISTIC_WRITE)
    @Query("select rt from RefreshToken rt where rt.token = :token")
    Optional<RefreshToken> findByTokenForUpdate(@Param("token") String token);

    // (opcional) revogar todos ativos do usuário (para “login em outro dispositivo”)
    @Query("""
        update RefreshToken rt
           set rt.revogado = true
         where rt.usuario.id = :usuarioId
           and rt.revogado = false
           and rt.expiracao > :agora
    """)
    @org.springframework.data.jpa.repository.Modifying
    int revogarTodosAtivosPorUsuario(@Param("usuarioId") Long usuarioId,
                                     @Param("agora") LocalDateTime agora);

}
