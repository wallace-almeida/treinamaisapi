package com.treinamaisapi.entity.simulado;

import com.treinamaisapi.entity.questoes_respondida.QuestaoSimulado;
import com.treinamaisapi.entity.usuarios.Usuario;
import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;
import java.util.List;

@Entity
@Table(name = "SIMULADOS")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Simulado {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    private String titulo; // opcional: "Simulado de Matemática", etc.

    @ManyToOne
    @JoinColumn(name = "usuario_id")
    private Usuario usuario;

    private Integer quantidadeQuestoes;

    private Integer tempoDuracao; // em minutos

    private LocalDateTime dataCriacao;

    private Double pontuacaoFinal; // calculada no resultado

    @OneToMany(mappedBy = "simulado", cascade = CascadeType.ALL)
    private List<QuestaoSimulado> questoes;

    // filtros usados na criação
    private Long temaId;
    private Long capituloId;
    private Long subcapituloId;
    private String nivelDificuldade;
    private String banca;

}