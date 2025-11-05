package com.treinamaisapi.entity.questoes;

import com.treinamaisapi.entity.enums.NivelDificuldade;
import com.treinamaisapi.entity.subCapitulo.Subcapitulo;

import jakarta.persistence.*;
import lombok.*;



@Table(name = "QUESTOES", indexes = {
        @Index(name = "idx_questao_nivel", columnList = "nivel_dificuldade"),
        @Index(name = "idx_questao_banca", columnList = "banca"),
        @Index(name = "idx_questao_subcapitulo", columnList = "subcapitulo_id")
})
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Entity
public class Questao {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(columnDefinition = "TEXT")
    private String enunciado;

    private String alternativaA;
    private String alternativaB;
    private String alternativaC;
    private String alternativaD;
    private String respostaCorreta; // Ex: "D"
    @Column(columnDefinition = "TEXT")
    private String explicacao;

    @Enumerated(EnumType.STRING)
    private NivelDificuldade nivelDificuldade;

    private String banca;

    @ManyToOne
    @JoinColumn(name = "subcapitulo_id")
    private Subcapitulo subcapitulo;
}

