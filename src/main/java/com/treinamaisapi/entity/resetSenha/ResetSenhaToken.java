package com.treinamaisapi.entity.resetSenha;

import com.treinamaisapi.entity.usuarios.Usuario;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@Entity
@Table(name = "reset_senha_token")
@Data
@NoArgsConstructor
@AllArgsConstructor
public class ResetSenhaToken {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    private String token;

    private String codigo;

    private LocalDateTime expiracao;

    private boolean usado = false;

    @ManyToOne
    private Usuario usuario;
}

