package com.treinamaisapi.common.dto.usuario.perfil;

import com.fasterxml.jackson.annotation.JsonIgnore;
import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;

@Data
public class AtualizarPerfilRequest {


    private String name;


    @Email
    private String email;

    @NotBlank
    private String senhaAtual;

    @JsonIgnore
    public boolean isVazio() {
        return name == null && email == null;
    }
    // getters e setters
}

