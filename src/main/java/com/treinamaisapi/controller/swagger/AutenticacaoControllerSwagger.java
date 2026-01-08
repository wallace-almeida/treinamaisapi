package com.treinamaisapi.controller.swagger;

import com.treinamaisapi.common.dto.auth.AuthResponse;
import com.treinamaisapi.common.dto.auth.LoginRequest;
import com.treinamaisapi.common.dto.auth.RefreshTokenRequest;
import com.treinamaisapi.common.dto.auth.resetSenha.ConfirmarCodigoRequest;
import com.treinamaisapi.common.dto.auth.resetSenha.RedefinirSenhaRequest;
import com.treinamaisapi.common.dto.auth.resetSenha.SolicitarResetRequest;
import com.treinamaisapi.common.dto.auth.resetSenha.TokenResponse;
import com.treinamaisapi.entity.usuarios.Usuario;
import org.springframework.http.ResponseEntity;


import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;

@Tag(name = "Autenticacao", description = "Autenticacao")
public interface AutenticacaoControllerSwagger {


    @PostMapping("/login")
    ResponseEntity<AuthResponse> login(@RequestBody LoginRequest request);

    @PostMapping("/refresh")
    ResponseEntity<AuthResponse> refresh(@RequestBody RefreshTokenRequest request);


    @PostMapping("/logout")
    ResponseEntity<Void> logout(@RequestBody RefreshTokenRequest request);


    @PostMapping("/solicitar")
    ResponseEntity<Void> solicitar(@RequestBody SolicitarResetRequest request);

    @PostMapping("/confirmar-codigo")
    ResponseEntity<TokenResponse> confirmarCodigo(@RequestBody ConfirmarCodigoRequest request);

    @PostMapping("/redefinir")
    ResponseEntity<Void> redefinir(@RequestBody RedefinirSenhaRequest request);


    @GetMapping("/teste-email")
    String testarEmail();
}
