package com.treinamaisapi.controller.autenticacao;

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
        RefreshToken refresh = new RefreshToken();
        refresh.setToken(token);
        refresh.setUsuario(usuario);
        refresh.setExpiracao(LocalDateTime.now().plus(validade));
        refresh.setRevogado(false);
        return repository.save(refresh);
    }

    public RefreshToken validar(String token) {
        RefreshToken refresh = repository.findByToken(token)
                .orElseThrow(() -> new BusinessException("Refresh token inválido"));

        if (refresh.isRevogado())
            throw new BusinessException("Refresh token revogado");

        if (refresh.getExpiracao().isBefore(LocalDateTime.now()))
            throw new BusinessException("Refresh token expirado");

        return refresh;
    }

    public void revogar(RefreshToken token) {
        token.setRevogado(true);
        repository.save(token);
    }

    public void revogarTodosDoUsuario(Usuario usuario) {
        List<RefreshToken> tokens = repository
                .findAllByUsuarioAndRevogadoFalse(usuario);

        tokens.forEach(t -> t.setRevogado(true));
        repository.saveAll(tokens);
    }


}

