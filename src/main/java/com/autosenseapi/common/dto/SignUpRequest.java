package com.autosenseapi.common.dto;

import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Size;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
public class SignUpRequest {
    @NotBlank(message = "O nome é obrigatório")
    private String name;

    @NotBlank(message = "O email é obrigatório")
    @Email(message = "Email inválido")
    private String email;

    @NotBlank(message = "A senha é obrigatória")
    @Size(min = 6, message = "A senha deve ter pelo menos 6 caracteres")
    private String password;

    @NotBlank(message = "O telefone é obrigatório")
    private String phone;

    @NotBlank(message = "O CPF é obrigatório")
    @Pattern(
            regexp = "\\d{11}|\\d{3}\\.\\d{3}\\.\\d{3}-\\d{2}",
            message = "CPF inválido. Use o formato 000.000.000-00 ou 00000000000")
    private String cpfUser;

    @Pattern(regexp = "PACIENTE|DENTISTA", message = "O tipo de usuário deve ser PACIENTE ou DENTISTA")
    private String userType;  // Representação como String para facilitar a conversão

}
