package com.treinamaisapi.service.autenticacao;


import com.treinamaisapi.common.dto.auth.AuthResponse;
import com.treinamaisapi.common.dto.auth.LoginRequest;
import com.treinamaisapi.common.dto.auth.RefreshTokenRequest;

import jakarta.transaction.Transactional;
import com.treinamaisapi.common.dto.usuario.UsuarioResponse;



import com.treinamaisapi.common.exception.InvalidCredentialsException;
import com.treinamaisapi.controller.autenticacao.RefreshTokenService;
import com.treinamaisapi.entity.refreshToken.RefreshToken;
import com.treinamaisapi.entity.usuarios.Usuario;
import com.treinamaisapi.jwt.JwtService;

import com.treinamaisapi.repository.UsuarioRepository;

import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;

import java.time.Duration;
import java.time.LocalDateTime;

import static java.time.LocalDate.now;


@Service
public class AuthenticationService {

    private final UsuarioRepository usuarioRepository;
    private final PasswordEncoder passwordEncoder;
    private final JwtService jwtService;
    private final RefreshTokenService refreshTokenService;

    public AuthenticationService(
            UsuarioRepository usuarioRepository,
            PasswordEncoder passwordEncoder,
            JwtService jwtService,
            RefreshTokenService refreshTokenService
    ) {
        this.usuarioRepository = usuarioRepository;
        this.passwordEncoder = passwordEncoder;
        this.jwtService = jwtService;
        this.refreshTokenService = refreshTokenService;
    }

    public AuthResponse login(LoginRequest request) {

        Usuario usuario = usuarioRepository.findByEmail(request.getEmail())
                .orElseThrow(() -> new InvalidCredentialsException("Credenciais inválidas"));

        if (!passwordEncoder.matches(request.getSenha(), usuario.getSenha()))
            throw new InvalidCredentialsException("Credenciais inválidas");

        refreshTokenService.revogarTodosDoUsuario(usuario);

        String accessToken = jwtService.generateAccessToken(usuario.getEmail());
        String refreshToken = jwtService.generateRefreshToken(usuario.getEmail());

        refreshTokenService.criar(
                refreshToken,
                usuario,
                Duration.ofDays(7)
        );

        return new AuthResponse(
                accessToken,
                refreshToken,
                UsuarioResponse.from(usuario)
        );

    }

    @Transactional
    public AuthResponse refresh(RefreshTokenRequest request) {

        // 1️⃣ Valida o refresh token no banco
        RefreshToken refresh = refreshTokenService.validar(request.getRefreshToken());

        // 2️⃣ Usa o usuário VINDO DO BANCO (fonte da verdade)
        Usuario usuario = refresh.getUsuario();
        String username = usuario.getEmail();

        // 3️⃣ Revoga o refresh token antigo (rotation)
        refreshTokenService.revogar(refresh);

        // 4️⃣ Cria novo refresh token
        String novoRefresh = jwtService.generateRefreshToken(username);
        refreshTokenService.criar(
                novoRefresh,
                usuario,
                Duration.ofDays(7)
        );

        // 5️⃣ Cria novo access token
        String novoAccess = jwtService.generateAccessToken(username);

        // 6️⃣ Retorna os novos tokens
        return new AuthResponse(
                novoAccess,
                novoRefresh,
                null
        );
    }


    public void logout(String refreshToken) {
        RefreshToken refresh = refreshTokenService.validar(refreshToken);
        refreshTokenService.revogar(refresh);
    }


}

