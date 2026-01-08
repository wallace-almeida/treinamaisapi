package com.treinamaisapi.controller.autenticacao;

import com.treinamaisapi.common.dto.auth.AuthResponse;
import com.treinamaisapi.common.dto.auth.LoginRequest;
import com.treinamaisapi.common.dto.auth.RefreshTokenRequest;
import com.treinamaisapi.common.dto.auth.resetSenha.ConfirmarCodigoRequest;
import com.treinamaisapi.common.dto.auth.resetSenha.RedefinirSenhaRequest;
import com.treinamaisapi.common.dto.auth.resetSenha.SolicitarResetRequest;
import com.treinamaisapi.common.dto.auth.resetSenha.TokenResponse;
import com.treinamaisapi.controller.swagger.AutenticacaoControllerSwagger;
import com.treinamaisapi.service.autenticacao.AuthenticationService;
import com.treinamaisapi.service.email.EmailService;
import com.treinamaisapi.service.reseteSenha.ResetSenhaService;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping(path = "/auth", produces = MediaType.APPLICATION_JSON_VALUE)
public class AutenticacaoController implements AutenticacaoControllerSwagger {


    final private AuthenticationService authenticationService;
    final private ResetSenhaService service;
    final private EmailService emailService;

    public AutenticacaoController(AuthenticationService authenticationService, ResetSenhaService service, EmailService emailService) {
        this.authenticationService = authenticationService;
        this.service = service;
        this.emailService = emailService;
    }


    @PostMapping("/login")
    @Override
    public ResponseEntity<AuthResponse> login(@RequestBody LoginRequest request) {
        return ResponseEntity.ok(authenticationService.login(request));
    }

    @PostMapping("/refresh")
    @Override
    public ResponseEntity<AuthResponse> refresh(@RequestBody RefreshTokenRequest request) {
        return ResponseEntity.ok(authenticationService.refresh(request));
    }

    @PostMapping("/logout")
    @Override
    public ResponseEntity<Void> logout(@RequestBody RefreshTokenRequest request) {
        authenticationService.logout(request.getRefreshToken());
        return ResponseEntity.noContent().build();
    }


    // reset da senha

    @PostMapping("/solicitar")
    @Override
    public ResponseEntity<Void> solicitar(@RequestBody SolicitarResetRequest request) {
        service.solicitarCodigo(request.email());
        return ResponseEntity.noContent().build();
    }

    @PostMapping("/confirmar-codigo")
    @Override
    public ResponseEntity<TokenResponse> confirmarCodigo(@RequestBody ConfirmarCodigoRequest request) {
        String token = service.confirmarCodigo(request.email(), request.codigo());
        return ResponseEntity.ok(new TokenResponse(token));
    }

    @PostMapping("/redefinir")
    @Override
    public ResponseEntity<Void> redefinir(@RequestBody RedefinirSenhaRequest request) {
        service.redefinirSenha(request.token(), request.novaSenha());
        return ResponseEntity.noContent().build();
    }

    @GetMapping("/teste-email")
    @Override
    public String testarEmail() {
        try {
            emailService.testarEnvio();
            return "✅ Email de teste enviado! Verifique sua caixa de entrada/spam.";
        } catch (Exception e) {
            e.printStackTrace();
            return "❌ Falha ao enviar email: " + e.getMessage();
        }
    }

}
