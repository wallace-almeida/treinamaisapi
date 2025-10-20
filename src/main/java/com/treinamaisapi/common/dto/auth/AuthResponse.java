package com.treinamaisapi.common.dto.auth;

import com.treinamaisapi.common.dto.usuario.UsuarioResponse;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@AllArgsConstructor
public class AuthResponse {
    private String token;
    private String refreshToken;
    private UsuarioResponse usuario;
}

