package com.treinamaisapi.service.AppVersion;


import com.treinamaisapi.common.dto.auth.AuthResponse;
import com.treinamaisapi.common.dto.auth.LoginRequest;
import com.treinamaisapi.common.dto.auth.RefreshTokenRequest;
import com.treinamaisapi.common.dto.usuario.UsuarioResponse;
import com.treinamaisapi.common.dto.versaoApp.AppVersionDTO;
import com.treinamaisapi.common.exception.InvalidCredentialsException;
import com.treinamaisapi.controller.autenticacao.RefreshTokenService;
import com.treinamaisapi.entity.appVersion.AppVersion;
import com.treinamaisapi.entity.refreshToken.RefreshToken;
import com.treinamaisapi.entity.usuarios.Usuario;
import com.treinamaisapi.jwt.JwtService;
import com.treinamaisapi.repository.AppVersionRepository;
import com.treinamaisapi.repository.UsuarioRepository;
import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;

import java.time.Duration;


@Service
@RequiredArgsConstructor
public class AppVersionService {

    private final AppVersionRepository repository;

    public AppVersionDTO getLatestVersion() {

        AppVersion version = repository
                .findTopByOrderByVersionCodeDesc()
                .orElseThrow(() -> new RuntimeException("Nenhuma versão cadastrada"));

        AppVersionDTO dto = new AppVersionDTO();

        dto.setVersion(version.getVersionName());
        dto.setApkUrl(version.getApkUrl());
        dto.setDescription(version.getDescription());
        dto.setForceUpdate(version.getForceUpdate());

        return dto;
    }

}

