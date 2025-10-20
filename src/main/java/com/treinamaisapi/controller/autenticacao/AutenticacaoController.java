package com.treinamaisapi.controller.autenticacao;

import com.treinamaisapi.common.dto.auth.AuthResponse;
import com.treinamaisapi.common.dto.auth.LoginRequest;
import com.treinamaisapi.common.dto.auth.RefreshTokenRequest;
import com.treinamaisapi.controller.swagger.AutenticacaoControllerSwagger;
import com.treinamaisapi.service.autenticacao.AuthenticationService;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping(path = "/auth", produces = MediaType.APPLICATION_JSON_VALUE)
public class AutenticacaoController implements AutenticacaoControllerSwagger {


    final private AuthenticationService authenticationService;

    public AutenticacaoController(AuthenticationService authenticationService) {
        this.authenticationService = authenticationService;

    }


    @PostMapping("/login")
    @Override
    public ResponseEntity<AuthResponse> login(@RequestBody LoginRequest request) {
        return ResponseEntity.ok(authenticationService.login(request));
    }

    @PostMapping("/refresh")
    @Override
    public ResponseEntity<AuthResponse> refresh(@RequestBody RefreshTokenRequest request) {
        return ResponseEntity.ok(authenticationService.refreshToken(request));
    }


}
