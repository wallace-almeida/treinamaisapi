package com.autosenseapi.jwt;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.NoArgsConstructor;


@Builder
@AllArgsConstructor
@NoArgsConstructor
public class AuthenticationRequest {

    private String usuario;
    String senha;
    String idSistema;

    public String getIdSistema() {
        return idSistema;
    }

    public String getSenha() {
        return senha;
    }

    public String getUsuario() {
        return usuario;
    }
}
