package com.treinamaisapi.entity.usuarios;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.treinamaisapi.entity.baralho.Baralho;
import com.treinamaisapi.entity.historico_estudo.HistoricoEstudo;
import com.treinamaisapi.entity.pacotes.PacoteComprado;
import com.treinamaisapi.entity.pontuacao.Pontuacao;
import com.treinamaisapi.entity.simulado.Simulado;
import jakarta.persistence.*;
import lombok.*;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;

import java.time.LocalDateTime;
import java.util.Collection;
import java.util.List;

@Entity
@Table(name = "USUARIOS")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Usuario implements UserDetails {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(nullable = false)
    private String nome;

    @Column(nullable = false, unique = true)
    private String email;

    @Column(nullable = false)
    private String senha;

    private String avatar;

    @CreationTimestamp
    private LocalDateTime dataCadastro;

    @UpdateTimestamp
    private LocalDateTime atualizadoEm;

    @OneToMany(mappedBy = "usuario", cascade = CascadeType.ALL, orphanRemoval = true)
    @JsonIgnore
    private List<Simulado> simulados;

    @OneToMany(mappedBy = "usuario", cascade = CascadeType.ALL, orphanRemoval = true)
    @JsonIgnore
    private List<Baralho> baralhos;

    @OneToMany(mappedBy = "usuario", cascade = CascadeType.ALL, orphanRemoval = true)
    @JsonIgnore
    private List<HistoricoEstudo> historicos;

    @OneToOne(mappedBy = "usuario", cascade = CascadeType.ALL)
    @JsonIgnore
    private Pontuacao pontuacao;

    @OneToMany(mappedBy = "usuario", cascade = CascadeType.ALL, orphanRemoval = true)
    @JsonIgnore
    private List<PacoteComprado> pacotesComprados;

    public Usuario(Long id) {
        this.id = id;
    }

    // 🔒 Métodos necessários do UserDetails

    @Override
    public Collection<? extends GrantedAuthority> getAuthorities() {
        return List.of(); // caso vá adicionar perfis/roles no futuro
    }

    @Override
    public String getPassword() {
        return senha;
    }

    @Override
    public String getUsername() {
        return email;
    }

    @Override
    public boolean isAccountNonExpired() {
        return true;
    }

    @Override
    public boolean isAccountNonLocked() {
        return true;
    }

    @Override
    public boolean isCredentialsNonExpired() {
        return true;
    }

    @Override
    public boolean isEnabled() {
        return true;
    }
}
