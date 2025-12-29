package com.treinamaisapi.entity.enums;

public enum AvatarPermitido {

    AVATAR_01("avatar_01"),
    AVATAR_02("avatar_02"),
    AVATAR_03("avatar_03"),
    AVATAR_04("avatar_04"),
    AVATAR_05("avatar_05"),
    AVATAR_06("avatar_06"),
    AVATAR_07("avatar_07"),
    AVATAR_08("avatar_08"),
    AVATAR_09("avatar_09");

    private final String codigo;

    AvatarPermitido(String codigo) {
        this.codigo = codigo;
    }

    public String getCodigo() {
        return codigo;
    }

    public static boolean isValido(String avatar) {
        for (AvatarPermitido a : values()) {
            if (a.codigo.equals(avatar)) {
                return true;
            }
        }
        return false;
    }
}

