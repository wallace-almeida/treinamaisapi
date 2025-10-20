package com.treinamaisapi.entity.pontuacao;

import com.treinamaisapi.entity.usuarios.Usuario;
import jakarta.persistence.*;
import lombok.*;

@Entity
@Table(name = "PONTUACOES")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Pontuacao {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    private Double total = 0.0;

    private Integer nivelAtual = 1;

    @OneToOne
    @JoinColumn(name = "usuario_id")
    private Usuario usuario;
}
