package com.treinamaisapi.entity.cartao;

import com.treinamaisapi.entity.baralho.Baralho;
import jakarta.persistence.*;
import lombok.*;
import java.time.LocalDateTime;

@Entity
@Table(name = "CARTOES")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Cartao {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    private String frente; // pergunta ou conceito

    @Column(columnDefinition = "TEXT")
    private String verso; // resposta ou explicação

    private boolean precisaRevisar;

    private LocalDateTime ultimaRevisao;

    @ManyToOne
    @JoinColumn(name = "baralho_id")
    private Baralho baralho;
}
