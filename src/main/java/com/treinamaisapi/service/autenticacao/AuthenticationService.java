package com.treinamaisapi.service.autenticacao;

import com.treinamaisapi.common.dto.auth.AuthResponse;
import com.treinamaisapi.common.dto.auth.LoginRequest;
import com.treinamaisapi.common.dto.auth.RefreshTokenRequest;
import com.treinamaisapi.common.dto.usuario.UsuarioRequest;
import com.treinamaisapi.common.dto.usuario.UsuarioResponse;
import com.treinamaisapi.entity.usuarios.Usuario;
import com.treinamaisapi.jwt.JwtService;
import com.treinamaisapi.repository.UsuarioRepository;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;



@Service
public class AuthenticationService {

    private final UsuarioRepository usuarioRepository;
    private final PasswordEncoder passwordEncoder;
    private final JwtService jwtService;

    public AuthenticationService(UsuarioRepository usuarioRepository, PasswordEncoder passwordEncoder, JwtService jwtService) {
        this.usuarioRepository = usuarioRepository;
        this.passwordEncoder = passwordEncoder;
        this.jwtService = jwtService;
    }

    public AuthResponse login(LoginRequest request) {
        Usuario usuario = usuarioRepository.findByEmail(request.getEmail())
                .orElseThrow(() -> new RuntimeException("Usuário não encontrado"));

        if (!passwordEncoder.matches(request.getSenha(), usuario.getSenha())) {
            throw new RuntimeException("Senha incorreta");
        }

        String accessToken = jwtService.generateAccessToken(usuario.getEmail());
        String refreshToken = jwtService.generateRefreshToken(usuario.getEmail());

        return new AuthResponse(
                accessToken,
                refreshToken,
                new UsuarioResponse(usuario.getId(), usuario.getNome(), usuario.getEmail() )
        );
    }

    public AuthResponse refreshToken(RefreshTokenRequest request) {
        String username = jwtService.extractUsername(request.getRefreshToken());

        if (!jwtService.isRefreshToken(request.getRefreshToken())) {
            throw new RuntimeException("O token enviado não é um refresh token");
        }

        if (!jwtService.isTokenValid(request.getRefreshToken(), username)) {
            throw new RuntimeException("Refresh token inválido ou expirado");
        }

        String newAccessToken = jwtService.generateAccessToken(username);

        return new AuthResponse(
                newAccessToken,
                request.getRefreshToken(), // opcionalmente você pode gerar um novo refresh token
                null
        );
    }
}

