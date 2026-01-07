package com.treinamaisapi.controller.swagger;

import com.treinamaisapi.common.dto.auth.AuthResponse;
import com.treinamaisapi.common.dto.auth.LoginRequest;
import com.treinamaisapi.common.dto.auth.RefreshTokenRequest;
import com.treinamaisapi.entity.usuarios.Usuario;
import org.springframework.http.ResponseEntity;


import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;

@Tag(name = "Autenticacao", description = "Autenticacao")
public interface AutenticacaoControllerSwagger {


    @PostMapping("/login")
    ResponseEntity<AuthResponse> login(@RequestBody LoginRequest request);

    @PostMapping("/refresh")
    ResponseEntity<AuthResponse> refresh(@RequestBody RefreshTokenRequest request);


    @PostMapping("/logout")
    ResponseEntity<Void> logout(@RequestBody RefreshTokenRequest request);
}
