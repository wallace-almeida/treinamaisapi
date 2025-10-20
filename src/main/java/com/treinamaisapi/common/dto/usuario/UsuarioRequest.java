package com.treinamaisapi.common.dto.usuario;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class UsuarioRequest {
    private String nome;
    private String email;
    private String senha;


}
/*@Pattern(
        regexp = "\\d{11}|\\d{3}\\.\\d{3}\\.\\d{3}-\\d{2}",
        message = "CPF inválido. Use o formato 000.000.000-00 ou 00000000000")
private String cpfUser;*/