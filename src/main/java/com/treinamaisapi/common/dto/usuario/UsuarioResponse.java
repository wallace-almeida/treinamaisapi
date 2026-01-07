package com.treinamaisapi.common.dto.usuario;

import com.treinamaisapi.entity.usuarios.Usuario;
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
    private String avatarCodigo;

    public static UsuarioResponse from(Usuario usuario) {
        return new UsuarioResponse(
                usuario.getId(),
                usuario.getNome(),        // ou getName(), ajuste conforme entidade
                usuario.getEmail(),
                usuario.getAvatar()       // ou avatarCodigo
        );
    }

}
