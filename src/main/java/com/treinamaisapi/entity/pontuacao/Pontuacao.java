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

    /**
     * XP total acumulado do usuário
     */
    @Column(nullable = false)
    private Double total = 0.0;

    /**
     * Nível atual calculado com base no total de XP
     */
    @Column(nullable = false)
    private Integer nivelAtual;

    @OneToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "usuario_id", nullable = false, unique = true)
    private Usuario usuario;

    public static Pontuacao nova(Usuario usuario) {
        return Pontuacao.builder()
                .usuario(usuario)
                .total(0.0)
                .nivelAtual(1)
                .build();
    }
}
