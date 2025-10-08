package com.autosenseapi.entity.usuarios.oficina;

import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;

import java.time.LocalDateTime;

@Entity
@Data
@Table(name = "usuarios_oficina")
@NoArgsConstructor
@AllArgsConstructor
public class UsuarioOficina {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name="ID")
    private Long id;

    @Column(nullable = false, length = 100, name = "NAME_RESPONSAVEL")
    private String nome_responsavel;

    @Column(nullable = false, length = 100, unique = true, name = "EMAIL")
    private String email;

    @Column(nullable = false, length = 100, name = "PASSWORD")
    private String senha;

    @Column(name = "FOTO_PERFIL")
    private String fotoPerfil;

    @CreationTimestamp
    @Column(name = "DATA_CADASTRO", updatable = false)
    private LocalDateTime dataCadastro;

    @UpdateTimestamp
    @Column(name = "ATUALIZADO_EM")
    private LocalDateTime atualizadoEm;

}
