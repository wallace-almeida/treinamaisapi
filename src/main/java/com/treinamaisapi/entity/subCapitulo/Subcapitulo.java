package com.treinamaisapi.entity.subCapitulo;

import com.treinamaisapi.entity.capitulo.Capitulo;
import com.treinamaisapi.entity.questoes.Questao;
import jakarta.persistence.*;
import lombok.Getter;
import lombok.Setter;

import java.util.ArrayList;
import java.util.List;

@Entity
@Table(name = "sub_capitulo")
@Getter
@Setter
public class Subcapitulo {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    private String nome; // Ex: "Equações do segundo grau"

    @ManyToOne
    @JoinColumn(name = "capitulo_id")
    private Capitulo capitulo;

    @OneToMany(mappedBy = "subcapitulo", cascade = CascadeType.ALL)
    private List<Questao> questoes = new ArrayList<>();
}
