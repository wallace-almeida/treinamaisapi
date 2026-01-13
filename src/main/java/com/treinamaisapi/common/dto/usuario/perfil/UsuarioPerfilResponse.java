package com.treinamaisapi.common.dto.usuario.perfil;

import com.treinamaisapi.entity.usuarios.Usuario;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class UsuarioPerfilResponse {

    private Long id;
    private String name;
    private String email;

    public static UsuarioPerfilResponse fromEntity(Usuario usuario) {
        return new UsuarioPerfilResponse(
                usuario.getId(),
                usuario.getNome(), // entidade pode continuar "nome"
                usuario.getEmail()
        );
    }
}
