package com.treinamaisapi.entity.questoes_respondida;

import com.treinamaisapi.entity.questoes.Questao;
import com.treinamaisapi.entity.simulado.Simulado;
import jakarta.persistence.*;
import lombok.*;

@Entity
@Table(name = "QUESTOES_RESPONDIDAS")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class QuestaoSimulado {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(optional = false)
    @JoinColumn(name = "simulado_id")
    private Simulado simulado;

    @ManyToOne(optional = false)
    @JoinColumn(name = "questao_id")
    private Questao questao;

    @Column(name = "resposta_usuario")
    private String respostaUsuario; // A, B, C, D (pode ser null antes de responder)

    @Column(name = "correta", nullable = true)
    private Boolean correta= null; // null = não respondida, true/false após responder

    @Column(name = "pontuacao_obtida")
    private Double pontuacaoObtida; // ex 1.0 / 0.0
}

