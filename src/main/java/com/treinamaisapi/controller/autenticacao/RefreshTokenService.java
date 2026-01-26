package com.treinamaisapi.controller.autenticacao;

import com.treinamaisapi.common.exception.AuthException;
import com.treinamaisapi.common.exception.BusinessException;
import com.treinamaisapi.entity.refreshToken.RefreshToken;
import com.treinamaisapi.entity.usuarios.Usuario;
import com.treinamaisapi.repository.RefreshTokenRepository;
import jakarta.transaction.Transactional;
import org.springframework.stereotype.Service;

import java.time.Duration;
import java.time.LocalDateTime;
import java.util.List;

@Service
@Transactional
public class RefreshTokenService {

    private final RefreshTokenRepository repository;

    public RefreshTokenService(RefreshTokenRepository repository) {
        this.repository = repository;
    }

    public RefreshToken criar(String token, Usuario usuario, Duration validade) {
        if (token == null || token.isBlank()) {
            throw new AuthException("REFRESH_MISSING", "Refresh token não informado.");
        }

        RefreshToken refresh = new RefreshToken();
        refresh.setToken(token);
        refresh.setUsuario(usuario);
        refresh.setExpiracao(LocalDateTime.now().plus(validade));
        refresh.setRevogado(false);
        return repository.save(refresh);
    }

    public RefreshToken validar(String token) {
        if (token == null || token.isBlank()) {
            throw new AuthException("REFRESH_MISSING", "Refresh token não informado.");
        }

        // 🔒 lock: evita duas requests rotacionarem o mesmo refresh ao mesmo tempo
        RefreshToken refresh = repository.findByTokenForUpdate(token)
                .orElseThrow(() -> new AuthException("REFRESH_NOT_FOUND", "Sessão inválida. Faça login novamente."));

        if (refresh.isRevogado()) {
            throw new AuthException("REFRESH_REVOKED", "Sessão encerrada. Faça login novamente.");
        }

        if (refresh.getExpiracao().isBefore(LocalDateTime.now())) {
            throw new AuthException("REFRESH_EXPIRED", "Sessão expirada. Faça login novamente.");
        }

        return refresh;
    }

    public void revogar(RefreshToken token) {
        if (token.isRevogado()) return; // idempotente
        token.setRevogado(true);
        repository.save(token);
    }

    // ✅ para “login em outro dispositivo”: mais eficiente usando update
    public void revogarTodosDoUsuario(Usuario usuario) {
        repository.revokeAllByUsuario(usuario.getId());
    }


    // (Se você preferir manter o método antigo com findAll/saveAll, pode.
    // Mas esse update é mais rápido.)
}

