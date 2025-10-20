package com.treinamaisapi.common.dto.usuario;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

@Setter
@Getter
@AllArgsConstructor
public class UsuarioResponse {
    private  Long id;
    private String name;
    private String email;

}
